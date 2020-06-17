--  { dg-do compile }
--  { dg-options "-O2" }

--  rpo fre3 used to loop indefinitely replacing _2 with _8 and back,
--  given MEM[(struct test__e &)_2][0]{lb: _7 sz: 16}._tag = A23s_29;
--  and an earlier _8 = &*_2[0]{lb: _7 sz: 16}.

procedure Opt83 is

   type E is tagged record
      I : Natural := 0;
   end record;

   type A is array (Natural range <>) of aliased E;

   F : E;

   R : access A;

   procedure N is
   begin
      if R = null then
        R := new A (0 .. 4);
      end if;
   end N;

begin

   N;

   R (0) := F;

end Opt83;
