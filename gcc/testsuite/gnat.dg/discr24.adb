-- { dg-do run }
-- { dg-options "-gnatp" }

procedure Discr24 is

   type Family_Type is (Family_Inet, Family_Inet6);
   type Port_Type is new Natural;

   subtype Inet_Addr_Comp_Type is Natural range 0 .. 255;

   type Inet_Addr_VN_Type is array (Natural range <>) of Inet_Addr_Comp_Type;

   subtype Inet_Addr_V4_Type is Inet_Addr_VN_Type (1 ..  4);
   subtype Inet_Addr_V6_Type is Inet_Addr_VN_Type (1 .. 16);

   type Inet_Addr_Type (Family : Family_Type := Family_Inet) is record
      case Family is
         when Family_Inet =>
            Sin_V4 : Inet_Addr_V4_Type := (others => 0);

         when Family_Inet6 =>
            Sin_V6 : Inet_Addr_V6_Type := (others => 0);
      end case;
   end record;

   type Sock_Addr_Type (Family : Family_Type := Family_Inet) is record
      Addr : Inet_Addr_Type (Family);
      Port : Port_Type;
   end record;

   function F return Inet_Addr_Type is
   begin
      return Inet_Addr_Type'
        (Family => Family_Inet, Sin_V4 => (192, 168, 169, 170));
   end F;

   SA : Sock_Addr_Type;

begin
   SA.Addr.Sin_V4 := (172, 16, 17, 18);
   SA.Port := 1111;
   SA.Addr := F;
   if SA.Port /= 1111 then
     raise Program_Error;
   end if;
end;
