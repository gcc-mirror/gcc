module b19002;

void printf(scope const char* format){}

void main()
{
    printf(__FILE__);
    printf(__FILE_FULL_PATH__);
    printf(__FUNCTION__);
    printf(__PRETTY_FUNCTION__);
    printf(__MODULE__);
}
