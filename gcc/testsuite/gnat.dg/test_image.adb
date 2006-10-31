-- { dg-do run }

with test_image_p;
procedure test_image is
  my_at5c : test_image_p.a_type5_class;
begin
  my_at5c := new test_image_p.type5;
end;
