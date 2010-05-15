with Lto9_Pkg2;

package Lto9_Pkg1 is

   subtype Lengths is Natural range 0 .. 50;

   type Subscriber (NLen, ALen: Lengths := 50) is record
      Name    : String(1 .. NLen);
      Address : String(1 .. ALen);
   end record;

   type Subscriber_Ptr is access all Subscriber;

   package District_Subscription_Lists is new Lto9_Pkg2
     (Element_Type => Subscriber,
      Element_Ptr  => Subscriber_Ptr,
      Size         => 100);

   District_01_Subscribers : District_Subscription_Lists.List_Type;

   New_Subscriber_01 : aliased Subscriber :=
     (12, 23, "Brown, Silas", "King's Pyland, Dartmoor");

end Lto9_Pkg1;
