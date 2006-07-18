-- { dg-do run }

procedure Outer_Agg_Bitfield_Constructor is

    type Mod_64 is mod 2 ** 64;
    for Mod_64'Size use 64;

    type Uint_16 is range 0 .. 2 ** 16 - 1;
    for Uint_16'Size use 16;

    type Values_Type is record
       M64 : Mod_64;
       U16 : Uint_16;
    end record;

    for Values_Type use record
       M64 at 0 range 0 .. 63;
       U16 at 8 range 0 .. 15;
    end record;

    type Wrapper_Type is record
       Values : Values_Type;
    end record;

    for Wrapper_Type use record
       Values at 0 range 0 .. 79;
    end record;

    M : constant := 2;
    U : constant := 4;

    W : Wrapper_Type := (Values => (M, U));

    procedure Check (O : Wrapper_Type) is
    begin
       if O.Values.M64 /= M or else O.Values.U16 /= U then
          raise Program_Error;
       end if;
    end;
begin
   Check (W);
end;


