/* { dg-do run } */

typedef __UINT8_TYPE__ u8;
typedef __INT8_TYPE__  i8;

#define NI __attribute__((noipa))

NI u8 mask_01_0 (void) { return __builtin_avr_mask1 (1, 0); }
NI u8 mask_01_1 (void) { return __builtin_avr_mask1 (1, 1); }
NI u8 mask_01_2 (void) { return __builtin_avr_mask1 (1, 2); }
NI u8 mask_01_3 (void) { return __builtin_avr_mask1 (1, 3); }
NI u8 mask_01_4 (void) { return __builtin_avr_mask1 (1, 4); }
NI u8 mask_01_5 (void) { return __builtin_avr_mask1 (1, 5); }
NI u8 mask_01_6 (void) { return __builtin_avr_mask1 (1, 6); }
NI u8 mask_01_7 (void) { return __builtin_avr_mask1 (1, 7); }
NI u8 mask_01_8 (void) { return __builtin_avr_mask1 (1, 8); }
NI u8 mask_01_9 (void) { return __builtin_avr_mask1 (1, 9); }

NI u8 mask_7f_0 (void) { return __builtin_avr_mask1 (0x7f, 0); }
NI u8 mask_7f_1 (void) { return __builtin_avr_mask1 (0x7f, 1); }
NI u8 mask_7f_2 (void) { return __builtin_avr_mask1 (0x7f, 2); }
NI u8 mask_7f_3 (void) { return __builtin_avr_mask1 (0x7f, 3); }
NI u8 mask_7f_4 (void) { return __builtin_avr_mask1 (0x7f, 4); }
NI u8 mask_7f_5 (void) { return __builtin_avr_mask1 (0x7f, 5); }
NI u8 mask_7f_6 (void) { return __builtin_avr_mask1 (0x7f, 6); }
NI u8 mask_7f_7 (void) { return __builtin_avr_mask1 (0x7f, 7); }
NI u8 mask_7f_8 (void) { return __builtin_avr_mask1 (0x7f, 8); }

NI u8 mask_80_0 (void) { return __builtin_avr_mask1 (0x80, 0); }
NI u8 mask_80_1 (void) { return __builtin_avr_mask1 (0x80, 1); }
NI u8 mask_80_2 (void) { return __builtin_avr_mask1 (0x80, 2); }
NI u8 mask_80_3 (void) { return __builtin_avr_mask1 (0x80, 3); }
NI u8 mask_80_4 (void) { return __builtin_avr_mask1 (0x80, 4); }
NI u8 mask_80_5 (void) { return __builtin_avr_mask1 (0x80, 5); }
NI u8 mask_80_6 (void) { return __builtin_avr_mask1 (0x80, 6); }
NI u8 mask_80_7 (void) { return __builtin_avr_mask1 (0x80, 7); }
NI u8 mask_80_8 (void) { return __builtin_avr_mask1 (0x80, 8); }

NI u8 mask_fe_0 (void) { return __builtin_avr_mask1 (0xfe, 0); }
NI u8 mask_fe_1 (void) { return __builtin_avr_mask1 (0xfe, 1); }
NI u8 mask_fe_2 (void) { return __builtin_avr_mask1 (0xfe, 2); }
NI u8 mask_fe_3 (void) { return __builtin_avr_mask1 (0xfe, 3); }
NI u8 mask_fe_4 (void) { return __builtin_avr_mask1 (0xfe, 4); }
NI u8 mask_fe_5 (void) { return __builtin_avr_mask1 (0xfe, 5); }
NI u8 mask_fe_6 (void) { return __builtin_avr_mask1 (0xfe, 6); }
NI u8 mask_fe_7 (void) { return __builtin_avr_mask1 (0xfe, 7); }
NI u8 mask_fe_8 (void) { return __builtin_avr_mask1 (0xfe, 8); }

NI u8 mask_0x01 (u8 x) { return __builtin_avr_mask1 (0x01, x); }
NI u8 mask_0x80 (u8 x) { return __builtin_avr_mask1 (0x80, x); }
NI u8 mask_0x7f (u8 x) { return __builtin_avr_mask1 (0x7f, x); }
NI u8 mask_0xfe (u8 x) { return __builtin_avr_mask1 (0xfe, x); }

NI u8 not_mask_0x01 (u8 x) { return ~ __builtin_avr_mask1 (0x01, x); }
NI u8 not_mask_0xfe (u8 x) { return ~ __builtin_avr_mask1 (0xfe, x); }
NI u8 not_mask_0x80 (u8 x) { return ~ __builtin_avr_mask1 (0x80, x); }
NI u8 not_mask_0x7f (u8 x) { return ~ __builtin_avr_mask1 (0x7f, x); }

NI u8 rotl (u8 x, u8 y)
{
  u8 i;
  for (i = 0; i < (7 & y); ++i)
    x = (x << 1) | (x >> 7);
  return x;
}

NI u8 rotr (u8 x, u8 y)
{
  u8 i;
  for (i = 0; i < (7 & y); ++i)
    x = (x >> 1) | (x << 7);
  return x;
}


NI u8 fun_0x01 (u8 x) { return rotl (0x01, x); }
NI u8 fun_0x80 (u8 x) { return rotr (0x80, x); }
NI u8 fun_0x7f (u8 x) { return rotr (0x7f, x); }
NI u8 fun_0xfe (u8 x) { return rotl (0xfe, x); }

NI u8 fun2r (u8 x)
{
  x &= 7;
  return 0x80 >> x;
}

NI u8 fun2l (u8 x)
{
  x &= 7;
  return 1 << x;
}

int main (void)
{
  i8 x;
  for (x = -10; x < 10; ++x)
    {
      if (mask_0x01 (x) != fun_0x01 (x)) __builtin_exit (__LINE__);
      if (mask_0xfe (x) != fun_0xfe (x)) __builtin_exit (__LINE__);
      if (mask_0x80 (x) != fun_0x80 (x)) __builtin_exit (__LINE__);
      if (mask_0x7f (x) != fun_0x7f (x)) __builtin_exit (__LINE__);

      if (not_mask_0x01 (x) != fun_0xfe (x)) __builtin_exit (__LINE__);
      if (not_mask_0xfe (x) != fun_0x01 (x)) __builtin_exit (__LINE__);
      if (not_mask_0x80 (x) != fun_0x7f (x)) __builtin_exit (__LINE__);
      if (not_mask_0x7f (x) != fun_0x80 (x)) __builtin_exit (__LINE__);

      if (fun2r (x) != fun_0x80 (x)) __builtin_exit (__LINE__);
      if (fun2l (x) != fun_0x01 (x)) __builtin_exit (__LINE__);
    }

  if (mask_01_0 () != mask_01_8 ())__builtin_exit (__LINE__);
  if (mask_fe_0 () != mask_fe_8 ())__builtin_exit (__LINE__);
  if (mask_80_0 () != mask_80_8 ())__builtin_exit (__LINE__);
  if (mask_7f_0 () != mask_7f_8 ())__builtin_exit (__LINE__);

  return 0;
}
