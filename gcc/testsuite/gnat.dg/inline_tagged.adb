-- { dg-do run }
-- { dg-options "-gnatN" }

with Text_IO; use Text_IO;
with system; use system; 
procedure inline_tagged is
   package Pkg is
      type T_Inner is tagged record
         Value : Integer;
      end record; 
      type T_Inner_access is access all T_Inner;
      procedure P2 (This : in T_Inner; Ptr : address);
      pragma inline (P2);
      type T_Outer is record
           Inner : T_Inner_Access;
      end record; 
      procedure P1 (This : access T_Outer);
   end Pkg;
   package body Pkg is
      procedure P2 (This : in T_Inner; Ptr : address) is
      begin   
         if this'address /= Ptr then
            raise Program_Error;
         end if;
      end;    
      procedure P1 (This : access T_Outer) is
      begin
         P2 (This.Inner.all, This.Inner.all'Address);
      end P1; 
   end Pkg;
   use Pkg;
   Thing : aliased T_Outer := (inner => new T_Inner);
begin   
   P1 (Thing'access);
end;    
