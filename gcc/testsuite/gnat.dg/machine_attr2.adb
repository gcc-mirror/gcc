-- { dg-do compile { target arm*-*-* } }
-- { dg-options "-mcpu=cortex-m33 -mcmse" }

package body Machine_Attr2 is
  
  procedure Call (Proc : Non_Secure) is
  begin
    Proc.all;
  end;

  procedure Foo; -- { dg-warning "only applies to base type" }
  pragma Machine_Attribute (Foo, "cmse_nonsecure_call");
  procedure Foo is null;

end Machine_Attr2;
