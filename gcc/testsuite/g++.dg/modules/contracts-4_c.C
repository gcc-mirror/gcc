// { dg-additional-options "-fmodules-ts -fcontracts -fcontract-role=default:ignore,ignore,ignore" }
export module bar;
import foo;

export int bar_fn_int(int n)
{
  return fn_t(n);
}

