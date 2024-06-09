#include <stdio.h>

#define INI_MAX_LINE 200

typedef char* (*ini_reader)(char* str, int num, void* stream);

int ini_parse(const char* filename);

static int ini_parse_stream(ini_reader reader, void* stream)
{
    char line[INI_MAX_LINE];
    int max_line = INI_MAX_LINE;
    while (reader(line, max_line, stream) != NULL)
	    ;
    return 0;
}

static int ini_parse_file(FILE* file)
{
    return ini_parse_stream((ini_reader)fgets, file);
}

int ini_parse(const char* filename)
{
    FILE* file;
    int error;

    file = fopen(filename, "r");
    if (!file)
        return -1;
    error = ini_parse_file(file);
    fclose(file);
    return error;
}
