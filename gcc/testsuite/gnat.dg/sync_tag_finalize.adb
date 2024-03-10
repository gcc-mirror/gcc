--  In previous versions of GNAT there was a curious bug that caused
--  compilation to fail in the case of a synchronized private extension
--  with non-default discriminants, where the creation of a constrained object
--  (and thus subtype) caused the TSS deep finalize machinery of the internal
--  class-wide constratined subtype (TConstrainedC) to construct a malformed
--  TSS finalize address body. The issue was that the machinery climbs
--  the type parent chain looking for a "non-constrained" type to use as a
--  designated (class-wide) type for a dispatching call to a higher TSS DF
--  subprogram. When there is a discriminated synchronized private extension
--  with known, non-default discriminants (thus unconstrained/indefinite), 
--  that search ends up at that private extension declaration. Since the
--  underlying type is actually a concurrent type, class-wide TSS finalizers
--  are not built for the type, but rather the corresponding record type. The
--  TSS machinery that selects the designated type was prevsiously unaware of
--  this caveat, and thus selected an incompatible designated type, leading to
--  failed compilation.
--
--  TL;DR: When creating a constrained subtype of a synchronized private
--  extension with known non-defaulted disciminants, the class-wide TSS
--  address finalization body for the constrained subtype should dispatch to
--  the corresponding record (class-wide) type deep finalize subprogram.

--  { dg-do compile }

procedure Sync_Tag_Finalize is
   
   package Ifaces is
      
      type Test_Interface is synchronized interface;
      
      procedure Interface_Action (Test: in out Test_Interface) is abstract;
      
   end Ifaces;
   
   
   package Implementation is
      type Test_Implementation
        (Constraint: Positive) is
        synchronized new Ifaces.Test_Interface with private;
      
   private
      protected type Test_Implementation
        (Constraint: Positive)
      is new Ifaces.Test_Interface with
      
         overriding procedure Interface_Action;
         
      end Test_Implementation;
   end Implementation;
   
   package body Implementation is
      protected body Test_Implementation is
         procedure Interface_Action is null;
      end;
   end Implementation;
   
   Constrained: Implementation.Test_Implementation(2);
begin
   null;
end;
