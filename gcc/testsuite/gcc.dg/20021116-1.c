/* { dg-do compile } */
/* { dg-options "-O2 -fpic" } */
/* { dg-warning "not supported" "PIC unsupported" { target cris-*-elf* cris-*-aout* mmix-*-* } 0 } */

void **
foo (void **x, int y, void *z)
{
  switch (y)
    {
    case 162:
      *x = z;
      break;
    case 164:
      *x = z;
      break;
    case 165:
      *x = z;
      break;
    case 166:
      *x = z;
      break;
    case 163:
      *x = z;
      break;
    default:
      goto out;
    }
  return x;

out:
  return (void **) 0;
}
