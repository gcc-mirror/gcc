// REQUIRED_ARGS: -g

class Retry
{
    alias bool delegate ( lazy void ) SuccessDecider;

    SuccessDecider success_decide;

    void on_retry ( )
    {
    }
}

