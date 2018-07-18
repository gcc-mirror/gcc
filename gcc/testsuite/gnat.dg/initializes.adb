--  { dg-do compile }

package body Initializes is
   protected body PO is
      procedure Proc is
         package Inner with Initializes => (Y => PO) is              --  OK
            Y : Boolean := X;
         end Inner;

         procedure Nested with Global => PO is                       --  OK
         begin
            null;
         end Nested;
      begin
         Nested;
      end Proc;
   end PO;

   protected body PT is
      procedure Proc is
         package Inner with Initializes => (Y => PT) is              --  OK
            Y : Boolean := X;
         end Inner;

         procedure Nested with Global => PT is                       --  OK
         begin
            null;
         end Nested;
      begin
         Nested;
      end Proc;
   end PT;
end Initializes;
