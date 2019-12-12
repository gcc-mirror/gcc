// PR c++/44648 - missing -Wunused warning on a const variable in if statement
// { dg-do compile }
// { dg-options "-Wunused" }

int main()
{
  bool b0 = 1;           // { dg-warning "\\\[-Wunused-variable\\\]" }
  const bool b00 = 1;    // { dg-warning "\\\[-Wunused-variable\\\]" }
  if (bool b1 = 1)       // { dg-warning "\\\[-Wunused-variable\\\]" }
    return 0;
  else
    return 1;

  if (const bool b2 = 1) // { dg-warning "\\\[-Wunused-variable\\\]" }
    return 0;
  else
    return 1;
}
