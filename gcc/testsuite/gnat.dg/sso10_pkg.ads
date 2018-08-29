pragma Default_Scalar_Storage_Order (High_Order_First);

package SSO10_Pkg is

  type Root is tagged null record;

  procedure Run (R : Root) is null;

end SSO10_Pkg;
