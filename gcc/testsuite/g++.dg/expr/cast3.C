// { dg-do compile }

enum MyState
{
        QUIT = 0,
        START,
        STOP,
        PAUSE
};

double GetDouble()
{
        return 1.0;
}

int main()
{
        MyState the_state;

        the_state = (MyState)GetDouble(); // { dg-bogus "invalid cast" }
        return 0;
}       
