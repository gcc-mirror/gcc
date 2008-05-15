-- { dg-do compile }
-- { dg-options "-gnatws" }

package body Discr8 is

  procedure Make (C : out Local_T) is
    Tmp : Local_T (Tag_One);
  begin
    C := Tmp;
  end;

  package Iteration is

    type Message_T is
      record
        S : Local_T;
      end record;

    type Iterator_T is
      record
        S : Local_T;
      end record;

    type Access_Iterator_T is access Iterator_T;

  end Iteration;

  package body Iteration is

    procedure Construct (Iterator : in out Access_Iterator_T;
                         Message  : Message_T) is
    begin
      Iterator.S := Message.S;
    end;

  end Iteration;

end Discr8;
