// Build don't link:
// Origin: philippeb@corel.com

class QObject
{
};


int main()
{
  QObject * o;
  int m;
  
  (o->*m)(); // ERROR - m cannot be used as a member pointer
}
