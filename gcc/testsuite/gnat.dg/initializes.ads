package Initializes is
   protected PO is
      procedure Proc;
   private
      X : Boolean := True;
   end PO;

   protected type PT is
      procedure Proc;
   private
      X : Boolean := True;
   end PT;
end Initializes;
