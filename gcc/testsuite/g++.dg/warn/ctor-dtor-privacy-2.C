struct C {
   static bool result;
private:
   static bool check();
};

bool C::result = check();
