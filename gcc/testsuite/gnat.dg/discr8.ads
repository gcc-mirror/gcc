with Discr8_Pkg1; use Discr8_Pkg1;

package Discr8 is

  type Tag_T is (Tag_One, Tag_Two);

  type Local_T (Tag : Tag_T := Tag_One) is
    record
      case Tag is
        when Tag_One =>
          A : T;
          B : Integer;
        when Tag_Two =>
          null;
      end case;
    end record;

  procedure Make (C : out Local_T);

end Discr8;
