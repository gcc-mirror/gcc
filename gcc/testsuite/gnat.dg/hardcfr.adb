--  { dg-do run }
--  { dg-options "-fharden-control-flow-redundancy -fno-hardcfr-check-exceptions -fdump-tree-hardcfr --param=hardcfr-max-blocks=22 --param=hardcfr-max-inline-blocks=12 -O0" }

procedure HardCFR is
   function F (I, J : Integer) return Integer is
   begin
      if (I < J) then
	 return 2 * I;
      else
	 return 3 * J;
      end if;
   end F;
   
   function G (I : Natural; J : Integer) return Integer is
   begin
      case I is
	 when 0 =>
	    return J * 2;
	   
	 when 1 =>
	    return J * 3;
	   
	 when 2 =>
	    return J * 5;
	   
	 when others =>
	    return J * 7;
      end case;
   end G;
   
   function H (I : Natural; -- { dg-warning "has more than 22 blocks, the requested maximum" }
	       J : Integer)
	       return Integer is
   begin
      case I is
	 when 0 =>
	    return J * 2;
	   
	 when 1 =>
	    return J * 3;
	   
	 when 2 =>
	    return J * 5;
	   
	 when 3 =>
	    return J * 7;

	 when 4 =>
	    return J * 11;
	   
	 when 5 =>
	    return J * 13;
	   
	 when 6 =>
	    return J * 17;
	   
	 when 7 =>
	    return J * 19;

	 when others =>
	    return J * 23;
      end case;
   end H;
begin
   if (F (1, 2) /= 2 or else F (3, 2) /= 6
	 or else G (2, 5) /= 25 or else H (4, 3) /= 33) 
   then
     raise Program_Error;
   end if;
end HardCFR;

--  HardCFR and HardCFR.F:
--  { dg-final { scan-tree-dump-times ".builtin_trap" 2 "hardcfr" } }

--  This is __builtin___hardcfr_check in HardCFR.G:
--  { dg-final { scan-tree-dump-times ".builtin " 1 "hardcfr" } }
