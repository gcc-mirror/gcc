// Build don't link:

template<class T>
void compute(T) {
    goto Exit;
Exit: ;
    }

int main() 
{
  compute(0);
}
