template <typename A1>
void monk2 (A1) {}

unsigned int strlen (const char*);

void monk ()
{
  monk2 (strlen (""));
}
