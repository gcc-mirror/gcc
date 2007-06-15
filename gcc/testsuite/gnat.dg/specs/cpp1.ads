--  { dg-do compile }

package cpp1 is
    type Root_Interface is interface; 

    type Typ is new Root_Interface with record
       TOTO : Integer;
       pragma CPP_Vtable (TOTO);
    end record;
end cpp1;
