/* { dg-do compile } */
/* { dg-options "-fcilkplus -Wno-return-type" } */

int func_2(void);

int check_spawn(int v)
{
  if (_Cilk_spawn func_2()) /* { dg-error "cannot contain" "" { target c } } */
  /* XXX: no error in C++ */
    ;
  if (v + _Cilk_spawn func_2())  /* { dg-error "cannot contain" "" { target c } } */
  /* { dg-error "invalid use" "" { target c++ } .-1 } */
    ;
  if (v, _Cilk_spawn func_2()) /* { dg-error "spawned function call cannot be part" } */
    ;
  v, _Cilk_spawn func_2(); /* { dg-error "spawned function call cannot be part" } */
  while (_Cilk_spawn func_2())  /* { dg-error "a condition for while statement" } */
    ;
  while (v + _Cilk_spawn func_2())  /* { dg-error "a condition for while statement" } */
    ;
  for (; _Cilk_spawn func_2() ;)  /* { dg-error "cannot be used" } */
    ;
  for (; v + _Cilk_spawn func_2() ;)  /* { dg-error "cannot be used" } */
    ;
  v + _Cilk_spawn func_2(); /* { dg-error } */
  for (_Cilk_spawn func_2() ;;)
    ;
  for (;; _Cilk_spawn func_2())
    ;
  do {} while(_Cilk_spawn func_2());  /* { dg-error "cannot be used" } */
  do {} while(v + _Cilk_spawn func_2());  /* { dg-error "cannot be used" } */
  switch (_Cilk_spawn func_2())   /* { dg-error "cannot be used" } */
    {
    default: break;
    }
  goto *(_Cilk_spawn func_2()); /* { dg-error "cannot be used" } */

  return _Cilk_spawn func_2(); /* { dg-error "is not allowed" } */
}

int check_array_notation(int x[100], int y[100])
{
  x[0:100] = y[0:100];
  for (; x[0:100] = y[0:100]; )  /* { dg-error "cannot be used" } */
    ;
  while (x[0:100] = y[0:100])  /* { dg-error "cannot be used" } */
    ;
  switch (x[0:100] = y[0:100])  /* { dg-error "cannot be used" } */
    {
      default: break;
    }
  do {} while (x[0:100] = y[0:100]);  /* { dg-error "cannot be used" } */
  if (x[0:100] = y[0:100]) /* allowed */
    ;
  return x[0:100] = y[0:100]; /* { dg-error "cannot be used" } */
}
