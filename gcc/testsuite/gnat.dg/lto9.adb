-- { dg-do run }
-- { dg-options "-flto" { target lto } }

with Lto9_Pkg1; use Lto9_Pkg1;

procedure Lto9 is

begin

   District_Subscription_Lists.Put
     (List     => District_01_Subscribers,
      Elem_Ptr => New_Subscriber_01'Access,
      Location => 1);

end;
