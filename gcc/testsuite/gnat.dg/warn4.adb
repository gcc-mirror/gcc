-- { dg-do compile }
-- { dg-options "-O2" }

with Unchecked_Conversion;

procedure Warn4 is

   type POSIX_Character is new Standard.Character;
   type POSIX_String is array (Positive range <>) of aliased POSIX_Character;

   type String_Ptr is access all String;
   type POSIX_String_Ptr is access all POSIX_String;

   function sptr_to_psptr is new Unchecked_Conversion -- { dg-warning "aliasing problem" }
     (String_Ptr, POSIX_String_Ptr); -- { dg-warning "" "" { target *-*-* } 14 }

   function To_POSIX_String (Str : String) return POSIX_String;
   function To_POSIX_String (Str : String)
      return POSIX_String is
   begin
      return sptr_to_psptr (Str'Unrestricted_Access).all;
   end To_POSIX_String;

   A : Boolean;
   S : String := "ABCD/abcd";
   P : Posix_String := "ABCD/abcd";

begin
   A := To_POSIX_String (S) = P;
end;
