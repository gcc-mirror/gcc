-- { dg-do run }

with GNAT.MD5; use GNAT.MD5;
procedure md5_test is
   TEST7  : constant String := "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq";
        
   Expected : constant Message_Digest :=
      "8215ef0796a20bcaaae116d3876c664a";
   MD : Context;
begin   
   Update (MD, TEST7);
   if Digest (MD) /= Expected then
      raise Program_Error;
   end if;
end;
