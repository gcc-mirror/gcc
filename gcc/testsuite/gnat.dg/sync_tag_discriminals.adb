-- This test is related to sync_tag_limited in that previous versions of GNAT
-- failed to consider a synchronized private extension as limited if it was
-- not derrived from a synchronized interface (i.e. a limited interface). Since
-- such a private type would not be considered limited, GNAT would fail to
-- correctly build the expected discriminals later needed by the creation of
-- the concurrent type's "corresponding record type", leading to a compilation
-- error where the discriminants of the corresponding record type had no
-- identifiers.
--
-- This test is in addition to sync_tag_limited because the sync_tag_limited
-- would fail for "legality" reasons (default discriminants not allowed for
-- a non-limited taged type). It is also an opportunity to ensure that non-
-- defaulted discriminated synchronized private extensions work as expected.

--  { dg-do compile }

procedure Sync_Tag_Discriminals is
   
   package Ifaces is
      
      type Test_Interface is limited interface;
      
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
   
begin
   null;
end Sync_Tag_Discriminals;
