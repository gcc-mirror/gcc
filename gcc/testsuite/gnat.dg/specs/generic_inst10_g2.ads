with Ada.Containers.Indefinite_Ordered_Maps;
with Generic_Inst10_G1;

generic
package Generic_Inst10_G2 is

  generic
    with package Actual_Types is new Generic_Inst10_G1;
  package Holder is
  private
    package Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (Integer, Integer);
  end Holder;

  generic
    with package Actual_Types is new Generic_Inst10_G1;
    with package Actual_Holder is new Holder (Actual_Types);
  package User is
  end User;

  generic
    with package Actual_Types is new Generic_Inst10_G1;
    with package Actual_User is
      new User (Actual_Types => Actual_Types, others => <>);
  package Final is
  end Final;

end Generic_Inst10_G2;
