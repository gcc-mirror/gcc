-- { dg-do run }
procedure Prot_Def is

   protected Prot is
      procedure Inc;
      function Get return Integer;
   private
      Data : Integer := 0;
   end Prot;

   protected body Prot is
      procedure Inc is
      begin
         Data := Data + 1;
      end Inc;
      function Get return Integer is
      begin
         return Data;
      end Get;
   end Prot;

   generic
      with procedure Inc is Prot.Inc;
      with function Get return Integer is Prot.Get;
   package Gen is
      function Add2_Get return Integer;
   end Gen;

   package body Gen is
      function Add2_Get return Integer is
      begin
         Inc;
	 Inc;
	 return Get;
      end Add2_Get;
   end Gen;

   package Inst is new Gen;

begin
   if Inst.Add2_Get /= 2 then
      raise Constraint_Error;
   end if;
end Prot_Def;
