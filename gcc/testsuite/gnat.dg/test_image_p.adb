with ada.task_identification;
with ada.text_io; use ada.text_io;
package body test_image_p is
    function to_type1 (arg1 : in Integer) return type1 is
    begin
        return  (f2 => (others => Standard.False));
    end to_type1;
    task body task_t is
       Name : String :=
             ada.task_identification.image (arg.the_task'identity);
    begin
        arg.the_array := (others => to_type1 (-1));
        if Name (1 .. 19) /= "my_at5c.f3.the_task" then
           Put_Line ("error");
           raise Program_Error;
        end if;
        
        select
           accept entry1;
        or 
           terminate;
        end select;
    end task_t;
end;
