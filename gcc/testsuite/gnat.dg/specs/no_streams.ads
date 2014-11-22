-- { dg-do compile }

pragma Restrictions (No_Streams);

with Ada.Containers.Ordered_Maps;

package No_Streams is

  type Arr is new String (1..8);

  package My_Ordered_Map is new Ada.Containers.Ordered_Maps
                                  (Key_Type => Natural, Element_Type => Arr);

end No_Streams;
