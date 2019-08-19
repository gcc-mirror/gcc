generic

  type Index_T is range <>;

package Inline18_Gen3 is

  generic
  package Inner_G is
    function Next (Position : Index_T) return Index_T;
    pragma Inline (Next);
  end Inner_G;

end Inline18_Gen3;
