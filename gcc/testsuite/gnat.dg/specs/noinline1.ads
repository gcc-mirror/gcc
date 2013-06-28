-- { dg-do compile }

package Noinline1 is

  procedure Proc1;
  pragma Inline (Proc1);
  pragma No_Inline (Proc1); -- { dg-warning "both specified" }

  procedure Proc2;
  pragma No_Inline (Proc2);
  pragma Inline (Proc2); -- { dg-warning "both specified" }

  procedure Proc3;
  pragma Inline_Always (Proc3);
  pragma No_Inline (Proc3); -- { dg-error "mutually exclusive" }

  procedure Proc4;
  pragma No_Inline (Proc4);
  pragma Inline_Always (Proc4); -- { dg-error "mutually exclusive" }

end Noinline1;
