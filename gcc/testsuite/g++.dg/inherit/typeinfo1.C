typedef struct {
   virtual const char *blah() {
     return "Heya::blah";
   }
} Heya;

struct Grok : public Heya {
   virtual const char *blah() {
     return "Grok::blah";
   }
};

int main() {
   Grok *g = new Grok();
   delete g;
   return 0;
}

