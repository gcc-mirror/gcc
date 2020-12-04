with Opt91_Pkg; use Opt91_Pkg;

package Opt91 is

   type True_Relation_Rec is null record;
   function Custom_Image (Self : True_Relation_Rec) return String;

   package True_Relation is new Pure_Relation (Ty => True_Relation_Rec);

end Opt91;
