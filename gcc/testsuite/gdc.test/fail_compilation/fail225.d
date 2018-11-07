struct Struct { 
        char* chptr; 
}

void main()
{
        char ch = 'd';
        immutable Struct iStruct = {1, &ch};
}

