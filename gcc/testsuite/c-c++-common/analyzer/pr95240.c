typedef __SIZE_TYPE__ size_t;

extern void *calloc(size_t nmemb, size_t size);
extern void free(void *ptr);

static char *activeTroubleArray;

int
initActiveTroubleArray ()
{
  activeTroubleArray = (char *) calloc (1, 1);
  return activeTroubleArray ? 0 : 1;
}

void
freeActiveTroubleArray ()
{
  free (activeTroubleArray);
}

int main (int argc, char *argv[])
{
  initActiveTroubleArray ();
  freeActiveTroubleArray ();

  return 1;
}
