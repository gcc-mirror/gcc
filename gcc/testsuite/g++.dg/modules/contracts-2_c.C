// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fcontracts -fcontract-role=default:maybe,maybe,ignore" }
import foo;

int main(int, char**)
{
  int x = -1;

  fn_iso(x--);
  fn1(x--);
  fn2(x--);
  fn3(x--);
  fn_in1(x--);
  fn_in2(x--);
  return (violation_count == 6 && violation_line_sum == 729) ? 0 : -1;
}

// TODO does any of this actually get verified?
// { dg-output "pre_print.-1.(\n|\r\n|\r)*" }
// { dg-output "violation: 1 12(\n|\r\n|\r)*" }
// { dg-output "fn_iso.-1.(\n|\r\n|\r)*" }

