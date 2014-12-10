-- { dg-do compile }
-- { dg-options "-O3" }

procedure Opt45 is

  type Index_T is mod 2 ** 32;
  for Index_T'Size use 32;
  for Index_T'Alignment use 1;

  type Array_T is array (Index_T range <>) of Natural;
  type Array_Ptr_T is access all Array_T;

  My_Array_1 : aliased Array_T := (1, 2);
  My_Array_2 : aliased Array_T := (3, 4);

  Array_Ptr : Array_Ptr_T := null;
  Index : Index_T := Index_T'First;

  My_Value : Natural := Natural'First;

  procedure Proc (Selection : Positive) is
  begin
    if Selection = 1 then
      Array_Ptr := My_Array_1'Access;
      Index := My_Array_1'First;
    else
      Array_Ptr := My_Array_2'Access;
      Index := My_Array_2'First;
    end if;

    if My_Value = Natural'First then
      My_Value := Array_Ptr.all (Index);
    end if;
  end;

begin
  Proc (2);
end;
