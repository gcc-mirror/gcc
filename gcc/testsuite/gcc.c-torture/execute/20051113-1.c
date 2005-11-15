extern void *malloc(__SIZE_TYPE__);
extern void *memset(void *, int, __SIZE_TYPE__);
typedef struct
{
  short a;  
  unsigned short b;
  unsigned short c;
  unsigned long long Count;
  long long Count2;
} __attribute__((packed)) Struct1;

typedef struct
{
  short a;
  unsigned short b;
  unsigned short c;
  unsigned long long d;
  long long e;
  long long f;
} __attribute__((packed)) Struct2;

typedef union
{
  Struct1 a;
  Struct2 b;
} Union;

typedef struct
{
  int Count;
  Union List[0];
} __attribute__((packed)) Struct3;

unsigned long long Sum (Struct3 *instrs) __attribute__((noinline));
unsigned long long Sum (Struct3 *instrs)
{
    unsigned long long  count = 0;
    int     i;
    
    for (i = 0; i < instrs->Count; i++) {
        count += instrs->List[i].a.Count;
    }
    return count;
}
long long Sum2 (Struct3 *instrs) __attribute__((noinline));
long long Sum2 (Struct3 *instrs)
{
    long long  count = 0;
    int     i;
    
    for (i = 0; i < instrs->Count; i++) {
        count += instrs->List[i].a.Count2;
    }
    return count;
}
main() {
  Struct3 *p = malloc (sizeof (int) + 3 * sizeof(Struct1));
  memset(p, 0, sizeof(int) + 3*sizeof(Struct1));
  p->Count = 3;
  p->List[0].a.Count = 555;
  p->List[1].a.Count = 999;
  p->List[2].a.Count = 0x101010101ULL;
  p->List[0].a.Count2 = 555;
  p->List[1].a.Count2 = 999;
  p->List[2].a.Count2 = 0x101010101LL;
  if (Sum(p) != 555 + 999 + 0x101010101ULL)
    abort();
  if (Sum2(p) != 555 + 999 + 0x101010101LL)
    abort();
  return 0;
}
