with Ada.Finalization;

package Opt68 is

  type Cont is new Ada.Finalization.Controlled with null record;

  type Element is record
    C : Cont;
  end record;

  type Queue_Element;
  type A_Queue_Element is access Queue_Element;
  type Queue_Element is record
    Value : Element;
    Next  : A_Queue_Element;
  end record;

  type Queue is limited record
    Sz    : Natural;
    Front : A_Queue_Element;
    Back  : A_Queue_Element;
  end record;

  procedure Copy (dest : in out Queue; src : Queue);

end Opt68;
