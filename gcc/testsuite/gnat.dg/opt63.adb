-- { dg-do compile }
-- { dg-options "-O -gnatws" }

procedure Opt63 is

   type T_MOD is mod 2**32;
   subtype T_INDEX is T_MOD range 3_000_000_000 .. 4_000_000_000;
   type T_ARRAY is array(T_INDEX range <>) of INTEGER;

   function Build_Crash(First : T_INDEX; Length : NATURAL) return T_ARRAY is
      R : T_ARRAY(First .. T_Index'Val (T_Index'Pos (First) + Length))
             := (others => -1); -- Crash here
   begin
      return R;
   end;

begin
   null;
end;
