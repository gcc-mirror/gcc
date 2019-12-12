-- { dg-do compile }

package body Prefetch1 is

  procedure Prefetch_1 (Addr : System.Address);
  pragma Import (Intrinsic, Prefetch_1, "__builtin_prefetch");

  procedure Prefetch_2 (Addr : System.Address; RW : Integer);
  pragma Import (Intrinsic, Prefetch_2, "__builtin_prefetch");

  procedure Prefetch_3 (Addr : System.Address; RW : Integer; Locality : Integer);
  pragma Import (Intrinsic, Prefetch_3, "__builtin_prefetch");

  procedure My_Proc1 (Addr : System.Address) is
  begin
    Prefetch_1 (Addr);
  end;

  procedure My_Proc2 (Addr : System.Address) is
  begin
    Prefetch_2 (Addr, 1);
  end;

  procedure My_Proc3 (Addr : System.Address) is
  begin
    Prefetch_3 (Addr, 1, 1);
  end;

end Prefetch1;
