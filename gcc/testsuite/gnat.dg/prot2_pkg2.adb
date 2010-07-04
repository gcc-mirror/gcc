with Unchecked_Deallocation;

package body Prot2_Pkg2 is

   protected type Rec is
   private
       M : T;
   end Rec;

   protected body Rec is end;

   procedure Create (B : out Id) is
   begin
       B := new Rec;
   end;

   procedure Delete (B : in out Id) is
      procedure Free is new Unchecked_Deallocation(Object => Rec, Name => Id);
   begin
      Free (B);
   end;

end Prot2_Pkg2;
