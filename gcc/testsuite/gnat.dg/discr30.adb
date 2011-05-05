-- PR ada/48844
-- Reported by Georg Bauhaus <bauhaus@futureapps.de> */

-- { dg-do compile }

procedure Discr30 is

   generic
     type Source is private;
     type Target is private;
   function Conversion (S : Source) return Target;

   function Conversion (S : Source) return Target is
      type Source_Wrapper is tagged record
         S : Source;
      end record;
      type Target_Wrapper is tagged record
         T : Target;
      end record;

      type Selector is (Source_Field, Target_Field);
      type Magic (Sel : Selector := Target_Field) is record
         case Sel is
            when Source_Field => S : Source_Wrapper;
            when Target_Field => T : Target_Wrapper;
         end case;
      end record;

      M : Magic;

      function Convert (T : Target_Wrapper) return Target is
      begin
         M := (Sel => Source_Field, S => (S => S));
         return T.T;
      end Convert;

   begin
      return Convert (M.T);
   end Conversion;

   type Integer_Access is access all Integer;

   I : aliased Integer;
   I_Access : Integer_Access := I'Access;

   function Convert is new Conversion (Integer_Access, Integer);

begin
   I := Convert (I_Access);
end;
