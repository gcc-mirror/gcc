
struct test_type 
{  
  int value;
  char *string;
};

void
callout (struct test_type *test_data)
{
  test_data->string = "ho there";
}

int main ()
{
  callout (&(struct test_type) { 3, "hey there" });
  exit (0);
}
