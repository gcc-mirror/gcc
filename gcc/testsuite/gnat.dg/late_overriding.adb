-- { dg-do compile }

procedure late_overriding is
   package Pkg is
      type I is interface;
      procedure Meth (O : in I) is abstract;
      type Root is abstract tagged null record; 
      type DT1 is abstract new Root and I with null record; 
   end Pkg;
   use Pkg;
   type DT2 is new DT1 with null record; 
   procedure Meth (X : DT2) is begin null; end;  --  Test
begin   
   null;   
end;
