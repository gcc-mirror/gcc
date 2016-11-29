package Debug7 is
   function Next (I : Integer) return Integer;
   function Renamed_Next (I : Integer) return Integer renames Next;
end Debug7;
