--  Synchronized tagged types created by a private extension with the keyword
--  'synchronized' shall be seen as an (immutably) limited tagged type, and
--  should therefore accept default disciminant spectifications.
--  This was a bug in earlier versions of GNAT, whereby GNAT erroneously
--  relied on a parent synchronized interface to determine limitedness
--  of a synchronized private extension. The problem being that a synchronized
--  private extension can derive a non-synchronized interface (specifically a
--  limited interface), Yet the RM makes it clear (7.3(6/2)) that such
--  synchronized private extensions are always limited.
--
--  Ergo: Default discriminants are of course legal on any synchronized private
--  extension.

--  { dg-do compile }

procedure Sync_Tag_Limited is
   
   package Ifaces is
      
      type Test_Interface is limited interface;
      
      procedure Interface_Action (Test: in out Test_Interface) is abstract;
      
   end Ifaces;
   
   
   package Implementation is
      type Test_Implementation
        (Constraint: Positive := 1) is
        synchronized new Ifaces.Test_Interface with private;
      
   private
      protected type Test_Implementation
        (Constraint: Positive := 1)
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
end Sync_Tag_Limited;
