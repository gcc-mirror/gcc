/* { dg-do run } */
/* { dg-options "-O1 -fno-inline" } */
/* { dg-require-effective-target int32plus } */

extern void abort (void);

static int something;

static int * converterData[2]={
    &something, &something,
};

static struct {
  const char *name;
  int type;
} const cnvNameType[] = {
  { "bocu1", 1 },
  { "utf7", 1 },
  { "utf8", 1 }
};


const int * getAlgorithmicTypeFromName(const char *realName);
const int *
getAlgorithmicTypeFromName(const char *realName)
{
    unsigned mid, start, limit;
    unsigned lastMid;
    int result;
    start = 0;
    limit = sizeof(cnvNameType)/sizeof(cnvNameType[0]);
    mid = limit;
    lastMid = 0xffffffff;

    for (;;) {
        mid = (start + limit) / 2;
        if (lastMid == mid) {   /* Have we moved? */
            break;  /* We haven't moved, and it wasn't found. */
        }
        lastMid = mid;
        result = __builtin_strcmp(realName, cnvNameType[mid].name);

        if (result < 0) {
            limit = mid;
        } else if (result > 0) {
            start = mid;
        } else {
            return converterData[cnvNameType[mid].type];
        }
    }

    return 0;
}

int main (void)
{
  if (!getAlgorithmicTypeFromName ("utf8"))
    abort ();
  return 0;
}
