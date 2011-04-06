package Debug2_Pkg is

    type String_Ptr is access all String;

    function To_Heap return String_Ptr;

    type String_List(Chars_Length: Positive) is private;

    type String_List_Ptr is access constant String_List;

    function Singleton return String_List;

private

    type String_List(Chars_Length: Positive) is record
        Chars: String(1..Chars_Length);
    end record;

end Debug2_Pkg;
