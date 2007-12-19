package test_image_p is
    type type1 is tagged private;
    type type3 is limited private;
    type type5 is tagged limited private;
    type a_type5_class is access all type5'Class;
    task type task_t (arg : access type3) is
        entry entry1;
    end task_t;
    function to_type1 (arg1 : in Integer) return type1;
private
  type array_t is array (Positive range <>) of type1;
  type array_t2 is array (1 .. 3) of Boolean;
  type type1 is tagged record
     f2 : array_t2;
  end record;
    type type3 is limited record
        the_task : aliased task_t (type3'Access);
        the_array : array_t (1 .. 10) := (others => to_type1 (-1));
    end record;
    type type5 is tagged limited record
        f3 : type3;
    end record;
end;
