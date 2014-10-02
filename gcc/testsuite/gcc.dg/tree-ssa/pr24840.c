/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef struct
{
  int tag, y;
} Object;

void Err_Handler () __attribute__ ((__noreturn__));
void Wrong_Type (Object, int);

int
P_Error (int argc, Object * argv)
{
    if (((argv[1]).tag >> 1) != 11)
      Wrong_Type (argv[1], 11);
  Err_Handler (argv[0], argv[1], argc - 2, argv + 2);
}
