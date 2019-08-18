package Prot7 is
   type Instance_Pointer is access Integer;

   protected Default_Slice
        with Lock_Free
   is
      function Get return Instance_Pointer;

      procedure Set (
        Discard : in out Boolean;
        Slice   : in     Instance_Pointer
      );
   private
      Default : Instance_Pointer;
   end Default_Slice;
end Prot7;
