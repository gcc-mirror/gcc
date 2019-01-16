module imports.test9919b;

class Event
{
    mixin genToString;  // @BUG@
}

class MouseEvent : Event
{
    enum Action { A, B }
}

mixin template genToString()
{
    override string toString()
    {
        return "";
    }
}
