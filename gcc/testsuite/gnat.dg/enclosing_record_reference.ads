package Enclosing_Record_Reference is
  pragma elaborate_body;

    type T is record
        F1: access function(x: integer) return T;
        F2: access function(x: T) return integer;             --??
        F3: access function(x: T) return T;                   --??
        F4: access function(x: integer) return access T;      --??
        F5: access function(x: access T) return integer;
        F6: access function(x: access T) return access T;
        F7: access function(x: T) return access T;            --??
        F8: access function(x: access T) return T;
    end record;

end Enclosing_Record_Reference;
