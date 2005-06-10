/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-m64 -O1 -static" } */
typedef unsigned long long uint64_t;

static int
match(name, pat)
 uint64_t *name, *pat;
{
 int ok=0, negate_range;
 uint64_t c, k;

  c = *pat++;
  switch (c & 0xffffffffffULL) {
  case ((uint64_t)(('[')|0x8000000000ULL)):
   if ((negate_range = ((*pat & 0xffffffffffULL) == ((uint64_t)(('!')|0x8000000000ULL)) )) != '\0')
    ++pat;
   while (((c = *pat++) & 0xffffffffffULL) )
    if ((*pat & 0xffffffffffULL) == ((uint64_t)(('-')|0x8000000000ULL))) 
      {
       pat += 2;
      } 

   if (ok == negate_range)
    return(0);
   break;
  }
 return(*name == '\0');
}

