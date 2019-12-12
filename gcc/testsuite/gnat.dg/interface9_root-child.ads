generic
package Interface9_Root.Child is
    type Base_Type is abstract new Base_Interface with null record;

    type Derived_Type is abstract new Base_Type and Derived_Interface
      with null record; --  Test
end Interface9_Root.Child;
