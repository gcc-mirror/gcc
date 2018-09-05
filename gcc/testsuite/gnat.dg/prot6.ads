package Prot6 is

   generic
      type TD is private;
      type TI is synchronized interface;
   package Set_Get is
      type T is synchronized interface and TI;

      procedure Set (E : in out T; D : TD) is abstract;
      function Get (E : T) return TD is abstract;
   end Set_Get;

   type My_Type_Interface is synchronized interface;

   package Set_Get_Integer is
     new Set_Get (TD => Integer,
                  TI => My_Type_Interface);
   use Set_Get_Integer;

   protected type My_Type is
        new Set_Get_Integer.T with

      overriding procedure Set (D : Integer);
      overriding function Get return Integer;
   private
      I : Integer;
   end My_Type;

   procedure Dummy;

end Prot6;
