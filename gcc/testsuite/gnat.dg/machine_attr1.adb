-- { dg-do compile { target i?86-*-linux* x86_64-*-linux* } }
-- { dg-options "-O3 -gnatp" }

package body Machine_Attr1 is

  procedure Proc1 is
  begin
    Proc3;
    Proc4;
  end;

  procedure Proc2 is
  begin
    Proc1;
  end;

  procedure Proc3 is
  begin
    A (1) := 0;
  end;

  procedure Proc4 is
  begin
    A (2) := 0;
  end;

  procedure Proc5 is
  begin
    for I in A'Range loop
      A(I) := B(I) + C(I);
    end loop;
  end;

  procedure Proc6 is
  begin
    for I in A'Range loop
      A(I) := B(I) + C(I);
    end loop;
  end;

end Machine_Attr1;
