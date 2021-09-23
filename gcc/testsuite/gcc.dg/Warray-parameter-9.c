/* PR c/99420 - bogus -Warray-parameter on a function redeclaration
   in function scope
   { dg-do compile }
   { dg-options "-Wall" } */

extern int a1[1], a2[2], a3[3], a4[4];

void fa1 (int [1]);     // { dg-message "previously declared as 'int\\\[1]'" }
void fa1 (int [1]);


void nested_decl (void)
{
  void fa2 (int [2]);

  fa2 (a1);             // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  fa2 (a2);
  fa2 (a3);

  void fa3 (int [3]);

  fa3 (a2);             // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  fa3 (a3);
}


void nested_redecl (void)
{
  void fa1 (int [2]);   // { dg-warning "argument 1 of type 'int\\\[2]' with mismatched bound" }

  fa1 (a1 + 1);         // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  fa1 (a1);

  void fa2 (int [2]);   // { dg-bogus "\\\[-Warray-parameter" }

  fa2 (a1);             // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  fa2 (a2);
  fa2 (a3);

  void fa3 (int [3]);   // { dg-bogus "\\\[-Warray-parameter" }

  fa3 (a2);             // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
  fa3 (a3);

  void fa4 (int [4]);
}

void fa4 (int [5]);     // { dg-warning "\\\[-Warray-parameter" }

void call_fa4 (void)
{
  fa4 (a4);
  fa4 (a3);             // { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" }
}
