/* PR tree-optimization/88753 */
/* { dg-options "-O2 -fdump-tree-switchconv" } */
/* { dg-do run { target nonpic } } */

typedef unsigned short int uint16_t;
typedef unsigned char uint8_t;

uint16_t length;
uint16_t enc_method_global;

uint8_t
__attribute__((noipa))
_zip_buffer_get_8(int buffer)
{
  return buffer;
}

  int
  __attribute__((noipa))
foo(int v)
{
  uint16_t enc_method;
  switch (_zip_buffer_get_8(v)) {
    case 1:
      enc_method = 0x0101;
      break;
    case 2:
      enc_method = 0x0102;
      break;
    case 3:
      enc_method = 0x0103;
      break;
    default:
      __builtin_abort ();
  }

  enc_method_global = enc_method;
}

int main(int argc, char **argv)
{
  foo (1);
  if (enc_method_global != 0x0101)
    __builtin_abort ();

  foo (2);
  if (enc_method_global != 0x0102)
    __builtin_abort ();

  foo (3);
  if (enc_method_global != 0x0103)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Linear transformation with A = 1 and B = 256" 1 "switchconv" } } */
