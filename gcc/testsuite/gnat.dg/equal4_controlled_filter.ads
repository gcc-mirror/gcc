with Equal4_Smart_Pointers;

generic
package Equal4_Controlled_Filter is
   type Object_T is private;

   function True return Object_T;

private
   package Smart is new Equal4_Smart_Pointers;

   type Object_T is new Smart.Pointer;
end Equal4_Controlled_Filter;
