with System.Address_To_Access_Conversions;

generic

  type T is new Root with private;

package Array5_Pkg2.G is

  package Ptr is new System.Address_To_Access_Conversions (T);

  Data : Ptr.Object_Pointer;

end Array5_Pkg2.G;
